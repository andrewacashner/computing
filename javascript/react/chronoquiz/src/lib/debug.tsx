const DEBUG = true;

export default function debug(msg: any): void {
  if (DEBUG) {
    console.debug(msg);
  }
}
