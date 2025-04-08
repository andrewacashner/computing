import java.util.Arrays;
import java.util.stream.*;

public class TreeNode<E extends Comparable<E>> {
    private E data;
    private TreeNode<E> left;
    private TreeNode<E> right;

    public TreeNode(E data) {
        this.data = data;
        this.left = null;
        this.right = null;
    }

    public TreeNode(E data, TreeNode<E> left, TreeNode<E> right) {
        this.data = data;
        this.left = left;
        this.right = right;
    }

    public static TreeNode<E> addSorted(TreeNode<E> root, TreeNode<E> node) {

        if (root == null) {
            root = node;
        } else {
            TreeNode<E> destination = 
                (node.data.compareTo(root.data) < 0) 
                ? root.left 
                : root.right;

            if (destination == null) {
                destination = node;
            } else {
                destination.addSorted(node);
            }
        }

        return root;
    }

    @Override
    public String toString() {
        return this.data.toString();
    }

    public String toStringInOrder() {
        String left = (this.left != null) ? this.left.toStringInOrder() : "";
        String root = this.toString();
        String right = (this.right != null) ? this.right.toStringInOrder() : "";
        return left + " " + root + " " + right;
    }

    public static void main(String[] args) {
        TreeNode<Integer> tree;
        Arrays.stream(args)
            .map(Integer::valueOf)
            .map(TreeNode<Integer>::new)
            .forEach(tree::addSorted);

        String sorted = tree.toStringInOrder();
        System.out.println(sorted);
    }
}
