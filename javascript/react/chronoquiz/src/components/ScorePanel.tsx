export default function ScorePanel({ score }: { score: number }) {
  return(
    <>
      <p>Score: <span className="score">{score}</span></p>
      <button id="restart">Play Again</button>
    </>
  );
}
