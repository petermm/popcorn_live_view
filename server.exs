Mix.install([
  {:bandit, "~> 1.1"},
  {:file_system, "~> 1.0"}
])

defmodule FileWatcher do
  def start(dirs) do
    spawn(fn ->
      {:ok, watcher} = FileSystem.start_link(dirs: dirs)
      FileSystem.subscribe(watcher)
      loop()
    end)
  end

  defp loop(timer \\ nil) do
    receive do
      {:file_event, _watcher, {_path, _events}} ->
        if timer, do: Process.cancel_timer(timer)
        loop(Process.send_after(self(), :cook, 1000))

      :cook ->
        IO.puts("\n[watcher] change detected, running mix cook...")
        System.cmd("mix", ["cook"], into: IO.stream(:stdio, :line), cd: File.cwd!())
        loop(nil)
    end
  end
end

defmodule Router do
  use Plug.Router

  @static_dir "#{__DIR__}/static"

  plug(Plug.Logger)
  plug(:add_headers)
  plug(Plug.Static, from: @static_dir, at: "/", gzip: true)
  plug(:match)
  plug(:dispatch)

  get "/" do
    send_file(conn, 200, "#{@static_dir}/index.html")
  end

  match _ do
    send_file(conn, 200, "#{@static_dir}/index.html")
  end

  def add_headers(conn, _opts) do
    Plug.Conn.merge_resp_headers(
      conn,
      [
        {"Access-Control-Allow-Origin", "*"},
        {"Cache-Control", "public no-cache"},
        {"Cross-Origin-Opener-Policy", "same-origin"},
        {"Cross-Origin-Embedder-Policy", "require-corp"}
      ]
    )
  end
end

{opts, _argv} = OptionParser.parse!(System.argv(), strict: [port: :integer])

FileWatcher.start(["lib"])

bandit = {Bandit, plug: Router, scheme: :http, port: Keyword.get(opts, :port, 4000)}
{:ok, _} = Supervisor.start_link([bandit], strategy: :one_for_one)

# unless running from IEx, sleep idenfinitely so we can serve requests
unless IEx.started?() do
  IO.getn("Press enter to exit\n")
end
