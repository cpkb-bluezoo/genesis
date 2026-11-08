/*
 * jimage.h
 * Reader of the JDK runtime image (lib/modules)
 * Copyright (C) 2026 Chris Burdess <dog@gnu.org>
 *
 * This file is part of genesis.
 *
 * genesis is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * genesis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#ifndef JIMAGE_H
#define JIMAGE_H

#include "platimage.h"

/*
 * Read the classes of a JDK runtime image - the "jimage" file lib/modules that
 * a JDK without its jmods directory keeps its platform classes in - into a
 * platform model.
 *
 * The image's index (header, location table, strings) is parsed into the model
 * and the file mapping is released before this returns. What stays behind is
 * the open file, from which the model reads a class when it is asked for it.
 *
 * modules is a NULL-terminated array of the names of the modules to keep, or
 * NULL for all of them. Returns NULL, having reported why on stderr, if the
 * file is not a jimage this reader understands.
 */
platform_image_t *jimage_load(const char *path, const char *const *modules);

#endif /* JIMAGE_H */
