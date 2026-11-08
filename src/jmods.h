/*
 * jmods.h
 * Reader of the JDK's jmods directory
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

#ifndef JMODS_H
#define JMODS_H

#include "platimage.h"

/*
 * Read the classes of a JDK's jmods directory into a platform model: one
 * .jmod file for each of the modules named. The zip directory of each file is
 * read into the model, and the file stays open for reading a class when it is
 * asked for.
 *
 * modules is a NULL-terminated array of module names. A module without a
 * file in the directory is left out. Returns NULL if none of them could be read.
 */
platform_image_t *jmods_load(const char *dir, const char *const *modules);

#endif /* JMODS_H */
