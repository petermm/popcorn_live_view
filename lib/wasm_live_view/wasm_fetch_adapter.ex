defmodule WasmLiveView.WasmFetchAdapter do
  @moduledoc """
  Kept as a thin alias around `Popcorn.Fetch` for call sites that still
  import this module. New OTP/BEAM Popcorn installs `Popcorn.Fetch` as
  Req's default adapter when Req is packed into the wasm image.
  """

  defdelegate run(request), to: Popcorn.Fetch
end
