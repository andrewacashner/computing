const DEBUG = false;

export default function debug(msg: any): void {
  if (DEBUG) {
    console.debug(msg);
  }
}
