import com.intellij.configurationStore.SerializableScheme
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.editor.colors.ColorKey
import com.intellij.openapi.editor.colors.EditorColors
import com.intellij.openapi.editor.colors.EditorColorsManager
import com.intellij.openapi.editor.colors.EditorColorsScheme
import com.intellij.openapi.editor.colors.TextAttributesKey
import com.intellij.openapi.editor.colors.impl.ReadOnlyColorsScheme
import com.intellij.openapi.editor.markup.EffectType
import com.intellij.openapi.editor.markup.TextAttributes
import com.intellij.ui.ColorUtil
import liveplugin.PluginUtil
import org.jdom.Element

import java.awt.Color
import java.awt.Font
import java.util.*

EditorColorsManager.getInstance().addColorsScheme(new RandomColorScheme(1,
        EditorColorsManager.getInstance().getScheme("_@user_Darcula")))
EditorColorsManager.getInstance().addColorsScheme(new RandomColorScheme(2,
        EditorColorsManager.getInstance().getScheme("_@user_Darcula")))
EditorColorsManager.getInstance().addColorsScheme(new RandomColorScheme(3,
        EditorColorsManager.getInstance().getScheme("_@user_Darcula")))

class RandomColorScheme implements EditorColorsScheme, SerializableScheme, ReadOnlyColorsScheme {
    Color[] availableColors = [
            ColorUtil.fromHex("A8C023"),
            ColorUtil.fromHex("287BDE"),
            ColorUtil.fromHex("589DF6"),
            ColorUtil.fromHex("BC3F3C"),
            ColorUtil.fromHex("CC7832"),
            ColorUtil.fromHex("6A8759"),
            ColorUtil.fromHex("81A36B"),
            ColorUtil.fromHex("9876AA"),
            ColorUtil.fromHex("629755"),
            ColorUtil.fromHex("8A653B"),
            ColorUtil.fromHex("FFC66D"),
            ColorUtil.fromHex("7DB4CA"),
            ColorUtil.fromHex("A390DA"),
            ColorUtil.fromHex("9AC69F"),
            ColorUtil.fromHex("BBB529"),
            ColorUtil.fromHex("9B9CFC"),
            ColorUtil.fromHex("6E76B4"),
            ColorUtil.fromHex("CC7832"),
            ColorUtil.fromHex("507874"),
            ColorUtil.fromHex("A9B7C6")
    ]
    Map<Long, Random> mapOfRandoms = new HashMap<>()
    @Delegate
    EditorColorsScheme delegate
    Random random = new Random(1337)
    List<Object> exceptions
    private int no

    RandomColorScheme(int no, EditorColorsScheme delegate) {
        this.no = no
        this.delegate = delegate
        if (delegate == null) {
            throw new IllegalArgumentException("delegate mustn't be null!")
        }
        exceptions = EditorColors.fields.collect { it.get(null) }
    }

    @Override
    String getName() {
        return "RandomColorScheme #" + no;
    }

    @Override
    String getDisplayName() {
        return "RandomColorScheme #" + no;
    }

    @Override
    Color getColor(ColorKey key) {
        if (key == null) return randomColor()
        return delegate.getColor(key)
    }

    @Override
    TextAttributes getAttributes(TextAttributesKey key) {
        if (key == null) return delegate.getAttributes(key)
        if (exceptions.contains(key)) return delegate.getAttributes(key)
        def attributes = delegate.getAttributes(key).clone()
        attributes.foregroundColor = randomColor()
        if (random.nextFloat() < 0.2) {
            attributes.effectType = EffectType.LINE_UNDERSCORE
            attributes.effectColor = attributes.foregroundColor
        }
        if (random.nextFloat() < 0.2) {
            attributes.fontType |= Font.BOLD
        }
        if (random.nextFloat() < 0.2) {
            attributes.fontType |= Font.ITALIC
        }
        return attributes
    }

    @Override
    Element writeScheme() {
        Element root = new Element("scheme")
        return root
    }

    private Color randomColor() {
        return availableColors[random.nextInt(availableColors.length)]
    }
}