/* Closest pair of points
 * Andrew Cashner
 * 2025/12/11
 */

#include <iostream>
#include <format>
#include <print>
#include <string>
#include <vector>
#include <cmath>

class Point
{
    public:
        double x;
        double y;

        Point() = default;
        Point(double x, double y): x { x }, y { y } {};

        std::string to_string() 
        {
            return std::format("({}, {})", x, y);
        }
        
        double distance_to(Point b);

        bool equals(Point b)
        {
            return this->x == b.x && this->y == b.y;
        }
};

double distance(std::pair<Point, Point> pair);

std::vector<Point> points { 
    Point(1, 2),
    Point(0.2, 0.5),
    Point(-0.1, -0.1),
    Point(-1, 4),
    Point(5, 2),
    Point(7, 0.1),
    Point(-0.02, 0),
    Point(9, 9),
    Point(-2, -.01),
    Point(-0.002, 1),
    Point(-0.001, 2.2),
    Point(-1, -1.1),
    Point(0, 0.2),
    Point(20, 4.5),
};

double distance(std::pair<Point, Point> pair);
std::pair<Point, Point> closest_pair(std::vector<Point> points);

int main(void)
{
    std::pair<Point, Point> pair { closest_pair(points) };
    std::print("Closest: [{}, {}]\n", pair.first.to_string(), pair.second.to_string());
    std::print("Distance = {}\n", pair.first.distance_to(pair.second));
}

std::pair<Point, Point> closest_pair(std::vector<Point> points)
{
    std::sort(points.begin(), points.end(), 
            [](Point p, Point q){ return p.x < q.x; });
  
    std::cout << "Input sorted by x\n";
    for (auto& point : points)
    {
        std::cout << point.to_string() << " ";
    }
    std::cout << "\n";

    if (points.size() == 3)
    {
        if (points[0].distance_to(points[1]) < 
                points[1].distance_to(points[2]))
        {
            return std::pair(points[0], points[1]);
        }
        else
        {
            return std::pair(points[1], points[2]);
        }
    }
    else if (points.size() == 2)
    {
        return std::pair(points[0], points[1]);
    }

    int midpoint { (int) points.size() / 2 };
    double median { points[midpoint].x };

    std::vector<Point> left { points.begin(), points.begin() + midpoint };
    std::vector<Point> right { points.begin() + midpoint, points.end() };

    std::pair<Point, Point> closest_left { closest_pair(left) };
    std::pair<Point, Point> closest_right { closest_pair(right) };
    std::print("Closest left: [{}, {}]\n", closest_left.first.to_string(),
            closest_left.second.to_string());;
    std::print("Closest right: [{}, {}]\n", closest_right.first.to_string(),
            closest_right.second.to_string());;

    double shortest_distance = std::min(distance(closest_left),
                                        distance(closest_right));

    std::vector<Point> midrange {};
    for (auto& point : points)
    {
        if (std::abs(median - point.x) < shortest_distance
                && !point.equals(closest_left.second)
                && !point.equals(closest_right.first))
        {
            midrange.push_back(point);
        }
    }

    std::pair<Point, Point> closest { std::pair(Point(), Point()) };
    bool middle_found = false;

    if (midrange.size() > 1)
    {
        std::sort(points.begin(), points.end(), 
                [](Point p, Point q){ return p.y < q.y; });

        std::cout << "Midrange sorted by y\n";
        for (auto& point : midrange)
        {
            std::cout << point.to_string() << " ";
        }
        std::cout << "\n";
    }

    if (midrange.size() == 2 
            && midrange[0].distance_to(midrange[1]) < shortest_distance)
    {
        closest = std::pair(midrange[0], midrange[1]);
    }
    else if (midrange.size() > 2)
    {
        Point current {};
        Point match {};

        for (long unsigned int i = 0; 
                !middle_found && i < midrange.size(); 
                ++i)
        {
            current = points[i];

            double min = shortest_distance;

            for (long unsigned int j = i + 1; 
                    !middle_found && j < 7 && j < midrange.size(); 
                    ++j)
            {
                match = points[j];
                middle_found = (current.distance_to(match) < min);
            }
        }
        if (middle_found)
        {
            closest = std::pair(current, match);
        }
    }

    if (!middle_found)
    {
        if (distance(closest_left) < distance(closest_right))
        {
            closest = closest_left;
        }
        else 
        {
            closest = closest_right;
        }
    }

    return closest;
}

double Point::distance_to(Point b)
{
    double x = this->x - b.x;
    double y = this->y - b.y;
    return std::sqrt(std::pow(x, 2) + std::pow(y, 2));
}

double distance(std::pair<Point, Point> pair)
{
    return pair.first.distance_to(pair.second);
}

