/*
    Node_ptr left = Node_create_from_data("left");
    Node_ptr right = Node_create_from_data("right");
    Node_ptr tree = Tree_create();
    tree = Tree_append_child(tree, left);
    left = Tree_append_sibling(left, right);

    Node_ptr left2 = Node_create_from_data("left2");
    tree = Tree_append_child(tree, left2);
    left2 = Tree_append_sibling(left2, Node_create_from_data("right2"));

    Tree_print_inorder(tree);
    Tree_delete(tree);
*/
void Tree_print_inorder_xml(Node_ptr root, int indent_level) {
    int shiftwidth = 2;
    Node_ptr this = root;
    if (this) {
        if (strlen(this->data) == 0) {
            printf("<root>");
        } else {
            printf("\n%*s<el>", indent_level, "");
        }

        Node_print(this);
       
        if (this->child) {
            Tree_print_inorder_xml(this->child, 
                    indent_level + shiftwidth);
        }

        if (strlen(this->data) == 0) {
            printf("</root>");
        } else {
            printf("</el>");
        }

        if (this->sibling) {
            Tree_print_inorder_xml(this->sibling, indent_level);
        }

        if (!this->child && !this->sibling) {
            printf("\n%*s", indent_level - shiftwidth, "");
        }

    }
}

void Tree_print_inorder_lisp(Node_ptr root) {
    Node_ptr this = root;
    if (this) {
        Node_print(this);
        if (this->child) {
            printf(" c[");
            Tree_print_inorder_lisp(this->child);
        }
        if (this->sibling) {
            printf(" s");
            Tree_print_inorder_lisp(this->sibling);
        } 
        if (this->child) {
            printf("]");
        }
    } 
}

void Tree_print_inorder(Node_ptr root) {
    Tree_print_inorder_lisp(root);
    printf("\n");
}


