defmodule WasmLiveView.Endpoint do
  @moduledoc """
  Minimal endpoint stub for LiveView running in WASM.
  Only implements the config/1 and subscribe/1 callbacks that
  Phoenix.Socket and LiveView.Channel need.
  """

  def config(:pubsub_server), do: WasmLiveView.PubSub
  def config(:secret_key_base), do: "wasm-local-not-a-real-secret-key-base-just-a-placeholder"
  def config(:live_view), do: []
  def config(:debug_errors), do: false
  def config(:check_origin), do: false
  def config(:code_reloader), do: false
  def config(:live_reload), do: nil
  def config(_), do: nil

  def subscribe(_topic), do: :ok

  @script_name (case System.get_env("SCRIPT_NAME") do
    nil -> []
    name -> String.split(name, "/", trim: true)
  end)

  def script_name, do: @script_name
end
