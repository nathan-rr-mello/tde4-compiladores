import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class TabSimb {
  private List<TS_entry> lista;

  public TabSimb() {
    lista = new ArrayList<TS_entry>();
  }

  public TabSimb(List<TS_entry> lista) {
    this.lista = lista;
  }

  public void insert(TS_entry nodo) {
    lista.add(nodo);
  }

  public void remove(String id) {
    lista.removeIf(e -> e.getId().equals(id));
  }

  public void listar() {
    int cont = 0;
    System.out.println("\n\nListagem da tabela de simbolos:\n");
    for (TS_entry nodo : lista) {
      System.out.println(nodo);
    }
  }

  public TS_entry pesquisa(String umId) {
    for (TS_entry nodo : lista) {
      if (nodo.getId().equals(umId)) {
        return nodo;
      }
    }
    return null;
  }

  public List<TS_entry> getLista() {
    return lista;
  }

  public List<TS_entry> getParams() {
    return lista.stream().filter(e -> e.getClasse() == ClasseID.NomeParam).toList();
  }
}
